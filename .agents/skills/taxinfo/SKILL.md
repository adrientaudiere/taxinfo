```markdown
# taxinfo Development Patterns

> Auto-generated skill from repository analysis

## Overview
This skill teaches you the core development patterns and conventions used in the `taxinfo` TypeScript codebase. You'll learn about file naming, import/export styles, testing patterns, and how to perform common workflows efficiently. This guide is ideal for contributors looking to maintain consistency and productivity in the project.

## Coding Conventions

### File Naming
- **Style:** snake_case
- **Example:**  
  ```plaintext
  tax_calculator.ts
  user_data_parser.ts
  ```

### Import Style
- **Relative imports are used throughout the codebase.**
- **Example:**
  ```typescript
  import { calculateTax } from './tax_calculator';
  ```

### Export Style
- **Named exports are preferred.**
- **Example:**
  ```typescript
  // In tax_calculator.ts
  export function calculateTax(income: number): number { ... }
  ```

### Commit Messages
- **Freeform, no strict prefixing.**
- **Average length:** ~50 characters
- **Example:**
  ```
  Fix calculation for negative income values
  ```

## Workflows

### Adding a New Feature
**Trigger:** When you need to introduce a new functionality  
**Command:** `/add-feature`

1. Create a new file using snake_case naming.
2. Implement the feature using TypeScript.
3. Use relative imports to include any dependencies.
4. Export your functions or constants using named exports.
5. Write corresponding tests in a `.test.ts` file.
6. Commit your changes with a clear, concise message.

### Writing and Running Tests
**Trigger:** When verifying code correctness  
**Command:** `/run-tests`

1. Create a test file matching the pattern `*.test.ts`.
2. Write tests for your modules and functions.
3. Use the project's preferred test runner (framework not specified; check project docs or package.json).
4. Run all tests to ensure correctness.

### Refactoring Existing Code
**Trigger:** When improving code structure or readability  
**Command:** `/refactor`

1. Identify the target file(s).
2. Apply changes while maintaining snake_case file naming.
3. Use relative imports for any new dependencies.
4. Update named exports as needed.
5. Update or add tests if behavior changes.
6. Commit with a descriptive message.

## Testing Patterns

- **Test Files:** Named with the pattern `*.test.ts`
- **Framework:** Not explicitly specified; check for test runner in project setup.
- **Example:**
  ```typescript
  // tax_calculator.test.ts
  import { calculateTax } from './tax_calculator';

  test('calculates tax for positive income', () => {
    expect(calculateTax(50000)).toBe(7500);
  });
  ```

## Commands
| Command        | Purpose                                 |
|----------------|-----------------------------------------|
| /add-feature   | Scaffold and implement a new feature    |
| /run-tests     | Run all tests in the codebase           |
| /refactor      | Refactor existing code safely           |
```