import { test, expect, Page } from '@playwright/test';

const BASE = 'https://annajchr.github.io/oxcaml/';

/**
 * Navigate to the Capture Go URL and start a new game configured for a
 * given number of captures.
 *
 * @param page - Page instance used to interact with the UI.
 * @param captures - Number of captures to play to (defaults to 1).
 */
async function navigateAndStartCaptureGo(page: Page, captures = 1) {
  await page.goto(BASE);
  await page.locator('input[type="number"]').fill(String(captures));
  await page.getByRole('button', { name: 'Start Game' }).click();
}
/**
 * Place a stone by board coordinates (row, col).
 *
 * Each `.intersection` element corresponds to a board intersection point.
 * This function calculates the correct index of the `.intersection` to click
 * based on the given row and column (0-indexed) and then performs a click
 * on that element.
 *
 * @param page - page instance used to interact with the UI.
 * @param row - board row placement (0 = top). Valid range: 0..18.
 * @param col - board column placement (0 = left). Valid range: 0..18.
 * @throws {Error} If `row` or `col` are outside the valid 0..18 range.
 */
async function placeStoneAt(page: Page, row: number, col: number) {
  const BOARD_SIZE = 19;
  if (row < 0 || row >= BOARD_SIZE || col < 0 || col >= BOARD_SIZE) {
    throw new Error(`placeStoneAt: row and col must be between 0 and ${BOARD_SIZE - 1}`);
  }
  const index = row * BOARD_SIZE + col;
  await page.locator('.intersection').nth(index).click();
}
test.describe('Capture Go UI Tests', () => {
  test('front page shows expected text and input', async ({ page }) => {
    await page.goto(BASE);

    // Expect frontpage to display text elements and input elements for
    // starting game to X captures.
    await expect(page.getByRole('heading', { name: 'Capture Go' })).toBeVisible();
    await expect(page.getByText(/Play to how many captures/i)).toBeVisible();
    await expect(page.locator('input[type="number"]')).toBeVisible();
    await expect(page.getByRole('button', { name: 'Start Game' })).toBeVisible();
  });

  test('starting game shows board and capture counters', async ({ page }) => {
    await navigateAndStartCaptureGo(page, /* captures= */ 1);

    // Board should appear
    await expect(page.locator('.go-board')).toBeVisible();

    // Displays capture counters initialized to 0, and turn indicator (initial turn is White)
    await expect(page.locator('.black-captures')).toHaveText(/Black captures: 0/);
    await expect(page.locator('.white-captures')).toHaveText(/White captures: 0/);
    await expect(page.locator('.turn-indicator')).toHaveText("White's Turn");
  });

  test('initial board is empty', async ({ page }) => {
    await navigateAndStartCaptureGo(page, /* captures= */1);

    // Explect no stones and all empty intersections.
    // ( There are 361 total intersections on a 19 x 19 go board. )
    await expect(page.locator('.go-stone')).toHaveCount(0);
    await expect(page.locator('.intersection')).toHaveCount(361);
  });

  test('placing a stone creates a stone', async ({ page }) => {
    await navigateAndStartCaptureGo(page, /* captures= */1);

    await placeStoneAt(page, /* row= */ 9, /* col= */ 9);

    // One stone should be present now.
    await expect(page.locator('.go-stone')).toHaveCount(1);
  });


  test('placing a stone changes turn', async ({ page }) => {
    await navigateAndStartCaptureGo(page, /* captures= */ 1);

    await placeStoneAt(page, /* row= */ 9, /* col= */ 9);

    // Turn should have changed from White -> Black
    await expect(page.locator('.turn-indicator')).toHaveText("Black's Turn");
  });

  test('placed stones alternate color based on player turn', async ({ page }) => {
    await navigateAndStartCaptureGo(page, /* captures= */ 1);
  
    // First move (White)
    await placeStoneAt(page, /* row= */ 9, /* col= */ 9);
    const firstPlayedStone = page.locator('.go-stone svg circle').first();
    await expect(firstPlayedStone).toHaveAttribute('fill', 'white');

    // Second move (Black)
    await placeStoneAt(page, /* row= */ 9, /* col= */ 10);
    const secondPlayedStone = page.locator('.go-stone svg circle').nth(1);
    await expect(secondPlayedStone).toHaveAttribute('fill', 'black');
  });
});

  // These tests validates that all move errors should produce expected error behavior:
  //  - an error banner indicating the error type to the user
  //  - a red outline around the cell that triggered the error
  test.describe('Capture Go Move Errors UI Tests', () => {
    test('space already filled error', async ({ page }) => {
      await navigateAndStartCaptureGo(page, /* captures= */ 1);

      // Place a stone then attempt to place on same intersection
      await placeStoneAt(page, /* row= */ 9, /* col= */ 9);
      await placeStoneAt(page, /* row= */ 9, /* col= */ 9);

      // Expect an error banner and the intersection to be marked as error
      await expect(page.locator('.move-error-banner')).toHaveText('Space already filled');
      const idx = 9 * 19 + 9;
      const classes = await page.locator('.intersection').nth(idx).getAttribute('class');
      expect(classes).toContain('cell-error');
    });

    test('self-capture violation error', async ({ page }) => {
      await navigateAndStartCaptureGo(page, /* captures= */ 1);

      // Build up surrounding stones so that the next player's placement would be self-capture.
      // White moves (we want Black to attempt the self-capture later):
      await placeStoneAt(page, /* row= */ 8, /* col= */ 9); // W
      await placeStoneAt(page, /* row= */ 0, /* col= */ 0); // B
      await placeStoneAt(page, /* row= */ 9, /* col= */ 8); // W
      await placeStoneAt(page, /* row= */ 0, /* col= */ 1); // B
      await placeStoneAt(page, /* row= */ 9, /* col= */ 10); // W
      await placeStoneAt(page, /* row= */ 0, /* col= */ 2); // B
      await placeStoneAt(page, /* row= */ 10, /* col= */ 9); // W

      // Now it's Black's turn; Black placing at (9,9) would be self-capture
      await placeStoneAt(page, /* row= */ 9, /* col= */ 9);

      await expect(page.locator('.move-error-banner')).toHaveText('Move would be self-capture');
      const idx = 9 * 19 + 9;
      const classes = await page.locator('.intersection').nth(idx).getAttribute('class');
      expect(classes).toContain('cell-error');
    });

    test('ko violation error', async ({ page }) => {
      await navigateAndStartCaptureGo(page, /* captures= */ 5);

      // Sequence of moves to get a Ko violation:
      await placeStoneAt(page, /* row= */ 0, /* col= */ 1); // White
      await placeStoneAt(page, /* row= */ 1, /* col= */ 1); // Black
      await placeStoneAt(page, /* row= */ 1, /* col= */ 0); // White
      await placeStoneAt(page, /* row= */ 2, /* col= */ 0); // Black
      await placeStoneAt(page, /* row= */ 1, /* col= */ 2); // White
      await placeStoneAt(page, /* row= */ 2, /* col= */ 2); // Black
      await placeStoneAt(page, /* row= */ 0, /* col= */ 3); // White
      await placeStoneAt(page, /* row= */ 3, /* col= */ 1); // Black
      // White captures Black at (1,1) by playing (2,1)
      await placeStoneAt(page, /* row= */ 2, /* col= */ 1);
      // Black attempts to re-capture at (1,1) -> should be Ko violation
      await placeStoneAt(page, /* row= */ 1, /* col= */ 1);

      await expect(page.locator('.move-error-banner')).toHaveText('Move violates Ko rule');
      const idx = 1 * 19 + 1;
      await expect(page.locator('.intersection').nth(idx)).toHaveAttribute('class', /cell-error/);
    });

    test('valid move after error clears error behavior from UI', async ({ page }) => {
      const errIdx = 9 * 19 + 9;
      await navigateAndStartCaptureGo(page, /* captures= */ 1);

      // Cause a "space already filled" error should trigger error behavior.
      await placeStoneAt(page, /* row= */ 9, /* col= */ 9);
      await placeStoneAt(page, /* row= */ 9, /* col= */ 9);
  
      await expect(page.locator('.move-error-banner')).toBeVisible();
      await expect(page.locator('.intersection').nth(errIdx)).toHaveAttribute('class', /cell-error/);

      // Play a legal move elsewhere should clear error behavior.
      await placeStoneAt(page, /* row= */ 0, /* col= */ 5);

      await expect(page.locator('.move-error-banner')).toBeHidden();
      await expect(page.locator('.intersection').nth(errIdx)).not.toHaveAttribute('class', /cell-error/);
    });
  });
